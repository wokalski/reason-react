open ReactLib;

type state = Hooks.Reducer.t(int) => Hooks.nil;

type action =
  | MyAction(string);

type renderedTree;

external makeTreeOpaque:
  React.elem('renderedTree) => React.elem(renderedTree) =
  "%identity";

let makeTreeOpaque = (() => makeTreeOpaque)();

type t = state => renderedTree;

let render = children => {
  React.Stateful(
    hooks => {
      let (state, send, hooks) =
        Hooks.reducer(
          ~initialState=0,
          (MyAction(next), _) => int_of_string(next),
          hooks,
        );
      (
        hooks,
        makeTreeOpaque(
          <Div
            onClick={e => print_string("divClicked!")}
            className={"MyReducer:" ++ string_of_int(state)}
          />,
        ),
      );
    },
  );
};
