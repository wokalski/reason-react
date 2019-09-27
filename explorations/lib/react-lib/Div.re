type event = unit;

type state = string;

type t('renderedTree) = Hooks.nil => 'renderedTree;

let domEventHandler = e => ();

let domStateToString = state => state;

let render =
    (
      ~onFocusLost: option(event => unit)=?,
      ~onClick: option(event => unit)=?,
      ~className: string="",
      children,
    ) =>
  React.Stateless(children);
