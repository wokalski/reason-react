open ReactLib;

open ReactLib.React.Types;

type state = Hooks.State.t(string) => Hooks.nil;

type renderedTree = (Div.t(Input.t), MyReducer.t);

type t = state => renderedTree;

let render = (~shouldControlInput, children, self) =>
  React.Stateful(
    hooks => {
      let (state, _, hooks) = Hooks.state("AppInstance", hooks);
      let (controlledState, _, hooks) =
        Hooks.state(
          {
            let hooks = Hooks.empty();
            let (_, _, hooks) =
              Hooks.state("haha I am controlling your input", hooks);
            Hooks.ofState(
              ~onStateDidChange=() => (),
              Some(Hooks.toState(hooks)),
            );
          },
          hooks,
        );
      let input = <Input init="initTxt" />;
      let input =
        !shouldControlInput
          ? input : React.control(input, ~state=controlledState);
      (
        hooks,
        <>
          <Div className="divRenderedByAppContainsInput"> input </Div>
          <MyReducer />
        </>,
      );
    },
  );
