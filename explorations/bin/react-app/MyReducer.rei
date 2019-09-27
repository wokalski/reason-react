open ReactLib;

type state;

type action;

type renderedTree;

type t = state => renderedTree;

let render: 'children => React.renderable(t);
