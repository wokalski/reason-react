let suppress = {contents: false};


let rec printInstanceCollection:
  type t. (~s: string=?, React.subtree(t)) => string =
  (~s="", subtree) => {
    let dNext = " " ++ s;
    React.(
      switch (subtree) {
      | EmptyInstance => "EmptyIntance"
      | Instance(n) => "Instance(" ++ printInstance(~s=dNext, n) ++ ")"
      | TwoInstances(n1, n2) =>
        "TwoInstances("
        ++ "\n"
        ++ dNext
        ++ printInstanceCollection(~s=dNext, n1)
        ++ ","
        ++ "\n"
        ++ dNext
        ++ printInstanceCollection(~s=dNext, n2)
        ++ "\n"
        ++ s
        ++ ")"
      | InstanceMap(lst) =>
        "InstanceMap("
        ++ String.concat(
             ",",
             List.map(printInstanceCollection(~s=dNext), lst.toList()),
           )
        ++ ")"
      }
    );
  }

and printInstance: type s sub. (~s: string=?, React.inst(s => sub)) => string =
  (~s="", n) => {
    let state = Hooks.printState(Some(n.hooks));
    let state: Obj.t = Obj.magic(state);
    React.(
      "{\n"
      ++ s
      ++ "  state: "
      ++ (
        if (Obj.is_int(state)) {
          string_of_int(Obj.magic(state): int);
        } else if (Obj.tag(state) === Obj.string_tag) {
          "\"" ++ String.escaped(Obj.magic(state): string) ++ "\"";
        } else {
          "-";
        }
      )
      ++ ",\n"
      ++ s
      ++ "  subtree: "
      ++ printInstanceCollection(~s=" " ++ s, n.subtree)
      ++ "\n"
      ++ s
      ++ "}"
    );
  };

let printInstance = printInstanceCollection;

let printSection = s =>
  if (suppress.contents) {
    ();
  } else {
    print_endline("\n\n" ++ s);
  };

let printRoot: type s a sub. (string, Root.t(s => sub)) => unit =
  (title, root) =>
    switch (suppress.contents, root.elemsAndInstance) {
    | (false, None) =>
      print_endline(title);
      print_endline("\n\n" ++ "<NotRendered>" ++ "\n");
    | (false, Some((elems, subtree))) =>
      print_endline("\n\n" ++ title ++ "\n");
      print_endline(printInstanceCollection(subtree));
    | (true, _) => ()
    };
