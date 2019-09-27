open ReactLib;

open React;

type state = Hooks.State.t(string) => Hooks.nil;

type action =
  | Click;

type renderedTree = Div.t(React.empty);

type t = state => renderedTree;

let render = (~txt="default", ~size: int, children): React.renderable(t) =>
  Stateful(
    hooks => {
      let (txt, _, hooks) = Hooks.state(txt, hooks);
      (hooks, <> <Div className="buttonClass" /> </>);
    },
  );
