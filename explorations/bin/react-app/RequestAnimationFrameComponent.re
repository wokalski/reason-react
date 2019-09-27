/*
/**
 * Demonstrates subscribing to external sources and how the
 * self.handler will allow updating in your tree, in an imperative,
 * non-immutable/functional manner.  In practice, you wouldn't use the state
 * initializer to perform side effects like the subscription to animation frame
 * in this example, and we'll provide a better hook for that.
 */
open ReactLib;

open React;

type action =
  | Tick;

/*
 * This is an example of where we really need the |? to be lazy. The default
 * expression has a side effect. This needs to be added to Reason proper.
 * I've made getDefault a thunk accordingly, in the meantime.
 */
let (|?) = (x, getDefault) =>
  switch (x) {
  | None => getDefault()
  | Some(x) => x
  };

/* This is the dummy built in event type, will be customizable later. */
let onRaf = e => Tick;

let initialStateGetter = (send, ()) => {
  let _ = RequestAnimationFrame.request(() => send(onRaf));
  "initialAnimationFrameSetup";
};

let render = (~txt="default", children) =>
  Stateful(
    hooks => {
      let (state, _, hooks) = Hooks.state(None, hooks);
      let (send, hooks) =
        React.eventHook(
          (action, send, inst) => {
            /* Reason knows this is a div instance because you used div JSX!!! */
            /* The shape of instances reflects the shape of the JSX */
            let TwoInstances(_, Instance({subtree: Instance(d)})) =
              inst.subtree;
            let (state, _) = Hooks.dropFirst(React.stateOf(d));
            let divStateStr = Div.domStateToString(state);
            ignore(RequestAnimationFrame.request(send(onRaf)));
            txt ++ "->animFiredWithDeepDivState(" ++ divStateStr ++ ")";
          },
          hooks,
        );
      let _ = state |? initialStateGetter(send);
      (
        hooks,
        <>
          <Div className="rafFirstDiv" />
          <Div className="rafSecond">
            <Div className={"rafDeepDiv" ++ string_of_int(Random.int(10))} />
          </Div>
        </>,
      );
    },
  );
*/
