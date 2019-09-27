open ReactLib;

type state = Hooks.State.t(string) => Hooks.nil;

type renderedTree = Div.t(React.empty);

type t = state => renderedTree;

let render = (~init="deafult", children) =>
  React.Stateful(
    hooks => {
      let (_, _, hooks) = Hooks.state(init, hooks);
      (hooks, <> <Div className="divRenderedByInput" /> </>);
    },
  );
