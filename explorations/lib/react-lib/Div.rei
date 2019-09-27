type state = string;

type event;

type t('renderedTree) = Hooks.nil => 'renderedTree;

let render:
  (
    ~onFocusLost: event => unit=?,
    ~onClick: event => unit=?,
    ~className: string=?,
    React.elem('renderedTree)
  ) =>
  React.stateless('renderedTree);

let domStateToString: state => string;
