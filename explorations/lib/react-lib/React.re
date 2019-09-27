module Types = {
  /* This is actually needed to ensure GADTs are inferred  */
  type empty =
    | Empty_;
  /*
   * Also create another form for splicing in nodes into otherwise fixed length
   * sets.
   */

  type elementMap('key, 'element) = {
    map: 'b. (('key, 'element) => 'b) => elementMap('key, 'b),
    merge3:
      'a 'b 'c.
      (
        ('key, option('element), option('a), option('b)) => option('c),
        elementMap('key, 'a),
        elementMap('key, 'b)
      ) =>
      elementMap('key, 'c),

    get: 'key => 'element,
    set: ('key, 'element) => elementMap('key, 'element),
    toList: unit => list('element),
  };

  type elem('t) =
    | Empty: elem(empty)
    | Element(renderable('s => 'sub)): elem('s => 'sub)
    | TwoElements(elem('t1), elem('t2)): elem(('t1, 't2))
    /*
     * Not an ordered map yet, but should be.
     */
    | ElementMap(elementMap('key, elem('t))): elem(elementMap('key, 't))
  /**
   * Instance subtree. Mirrors the shape of JSX, instead of just being a List.
   */
  and subtree('t) =
    | EmptyInstance: subtree(empty)
    | Instance(inst('s => 'sub)): subtree('s => 'sub)
    /* Having TwoInstances mirror the fact that TwoElements requires sub
     * elements, was probably overkill. */
    | TwoInstances(subtree('t1), subtree('t2)): subtree(('t1, 't2))
    | InstanceMap(elementMap('key, subtree('t)))
      : subtree(elementMap('key, 't))
  /*
   * These are just convenient shortcuts to specifiying the entire spec.  It just
   * makes it so you don't have to do a spread onto a record, since in
   * static-by-default trees, you don't need to define a record beforehand.
   */
  and componentSpec('t) = (Hooks.all('s), elem('sub))
  constraint 't = 's => 'sub
  /**
   * The result of applying props. Now the result is a function that just waits
   * for React to supply the state, and in turn returns the componentSpec.
   *
   * Ignore warning 62: Type constraints do not apply to GADT cases of variant types.
   */
  [@ocaml.warning "-62"]
  and renderable('t) =
    | Stateless(elem('sub)): renderable(Hooks.nil => 'sub)
    | Stateful(Hooks.t('a, 'a) => componentSpec('t))
  constraint 't = 'a => 'sub
  and stateless('sub) = renderable(Hooks.nil => 'sub)
  and inst('t) = {
    /* Memoized just for performance */
    replacer: replacer('t),
    /* Memoized just for performance*/
    subreplacer: subreplacer('sub),
    renderable: renderable('t),
    subtree: subtree('sub),
    elems: elem('sub),
    hooks: Hooks.state('hooks),
  }
  constraint 't = 'hooks => 'sub
  /*
   * A series of chained functions that forms a fishing line that each component
   * has a handle to. When invoked it can cause its own individual instance node
   * in the tree to be replaced in the *root* tree that it exists in! This is
   * totally type safe, and requires no dynamic casts, and no searching through
   * the tree.
   */
  /* Make it even more generalized on action than necessary to avoid GADT
   * errors. */
  and replacer('t) = (inst('t) => inst('t)) => unit
  constraint 't = 's => 'sub
  and subreplacer('sub) = (subtree('sub) => subtree('sub)) => unit;
};

include Types;

module Replacer = {
  let instance = (thisReplacer, instSwapper) =>
    thisReplacer((Instance(inst) as subtree) => {
      let next = instSwapper(inst);
      inst !== next ? Instance(next) : subtree;
    });
  let twoInstancesA = (thisReplacer, aSwapper) =>
    thisReplacer((TwoInstances(a, b) as subtree) => {
      let next = aSwapper(a);
      next === a ? subtree : TwoInstances(next, b);
    });
  let twoInstancesB = (thisReplacer, bSwapper) =>
    thisReplacer((TwoInstances(a, b) as subtree) => {
      let next = bSwapper(b);
      next === b ? subtree : TwoInstances(a, next);
    });
  let instanceMap = (thisReplacer, key, swapper) =>
    thisReplacer((InstanceMap(iLst) as subtree) => {
      let inst = iLst.get(key);
      let next = swapper(inst);
      next === inst ? subtree : InstanceMap(iLst.set(key, inst));
    });
};


let rec initialHooks:
  type a sub. (option(Hooks.state(a)), replacer(a => sub)) => Hooks.t(a, a) =
  (state, replacer) => {
    Hooks.ofState(state, ~onStateDidChange=() =>
      replacer(inst => {
        let hooksOut = inst.hooks;
        let flushedState = Hooks.flushPendingStateUpdates(hooksOut);
        reconcile(~flushedState, inst, inst.renderable);
      })
    );
  }

and init:
  type s sub. (replacer(s => sub), renderable(s => sub)) => inst(s => sub) =
  (replacer, renderable) => {
    /* Causes a chain reaction until it hits the root! */
    let subreplacer = subtreeSwapper =>
      replacer(inst => {
        let nextSubtree = subtreeSwapper(inst.subtree);
        inst.subtree !== nextSubtree ? {...inst, subtree: nextSubtree} : inst;
      });

    let (hooks, elems) =
      switch (renderable) {
      | Stateful(renderable) =>
        let initialHooks = initialHooks(None, replacer);
        renderable(initialHooks);
      | Stateless(subElems) => (Hooks.empty(), subElems)
      };
    {
      replacer,
      subreplacer,
      renderable,
      elems,
      hooks: Hooks.toState(hooks),
      subtree: initSubtree(subreplacer, elems),
    };
  }

and initSubtree: type sub. (subreplacer(sub), elem(sub)) => subtree(sub) =
  (thisReplacer, jsx) =>
    switch (jsx) {
    | Empty => EmptyInstance
    | Element(renderable) =>
      Instance(init(Replacer.instance(thisReplacer), renderable))
    | TwoElements(stateRendererA, stateRendererB) =>
      TwoInstances(
        initSubtree(Replacer.twoInstancesA(thisReplacer), stateRendererA),
        initSubtree(Replacer.twoInstancesB(thisReplacer), stateRendererB),
      )
    | ElementMap(elems) =>
      let initElem = (key, e) => {
        initSubtree(Replacer.instanceMap(thisReplacer, key), e);
      };
      let sub = elems.map(initElem);
      InstanceMap(sub);
    }

and reconcile:
  type s sub.
    (~flushedState: Hooks.state(s), inst(s => sub), renderable(s => sub)) =>
    inst(s => sub) =
  (~flushedState, inst, renderable) => {
    let curSubelems = inst.elems;
    let curHooks = inst.hooks;
    let (nextHooks, nextElems) =
      switch (renderable) {
      | Stateless(nextElems) => (curHooks, nextElems)
      | Stateful(renderable) =>
        let (h, e) =
          renderable(initialHooks(Some(flushedState), inst.replacer));
        (Hooks.toState(h), e);
      };
    {
      ...inst,
      renderable,
      hooks: nextHooks,
      elems: nextElems,
      subtree:
        reconcileSubtree(
          inst.subreplacer,
          inst.subtree,
          curSubelems,
          nextElems,
        ),
    };
  }

and reconcileSubtree:
  type sub.
    (subreplacer(sub), subtree(sub), elem(sub), elem(sub)) => subtree(sub) =
  (thisReplacer, subtree, prevJsx, jsx) =>
    switch (subtree, prevJsx, jsx) {
    | (EmptyInstance, Empty, Empty) => EmptyInstance
    | (Instance(i) as instance, Element(rPrev), Element(r)) =>
      /* No point memoizing based on return val of reconcile being === to i,
       * because it never will be if rPrev !== r. */
      false && r === rPrev ? instance : Instance(reconcile(i.hooks, i, r))
    | (
        TwoInstances(ia, ib),
        TwoElements(raPrev, rbPrev),
        TwoElements(ra, rb),
      ) =>
      TwoInstances(
        reconcileSubtree(
          Replacer.twoInstancesA(thisReplacer),
          ia,
          raPrev,
          ra,
        ),
        reconcileSubtree(
          Replacer.twoInstancesB(thisReplacer),
          ib,
          rbPrev,
          rb,
        ),
      )
    | (InstanceMap(iMap), ElementMap(eMapPrev), ElementMap(eMap)) =>
      let nextSeq =
        iMap.merge3(
          (key, itm, r, rPrev) => {
            let subreplacer = Replacer.instanceMap(thisReplacer, key);
            switch (itm, rPrev, r) {
            | (Some(itm), Some(r), Some(rPrev)) =>
              Some(reconcileSubtree(subreplacer, itm, rPrev, r))
            | (Some(_), Some(_), None) => None
            | (None, Some(r), Some(rPrev)) =>
              Some(initSubtree(subreplacer, r))
            | _ => invalid_arg("Unreachable")
            };
          },
          eMap,
          eMapPrev,
        );
      InstanceMap(nextSeq);
    };

/**
 * Seamless control of any stateful component!
 */
let control: (elem('s => 'sub), ~state: Hooks.t('s, 's)) => elem('s => 'sub) =
  (
    Element(renderable) as element,
    ~state as controlledState: Hooks.t('s, 's),
  ) =>
    switch (renderable) {
    | Stateful(f) => Element(Stateful(_ => f(controlledState)))
    | _ => element
    };

let stateOf = ({hooks}) => hooks;
