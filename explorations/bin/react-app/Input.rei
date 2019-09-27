/**
 * Demonstrates temporarily controlling an element that did not need to
 * anticipate being controlled! In this model, all components are controlled
 * components. They merely need to expose their state.
 * This input has no idea it is a controllable component.
 */
open ReactLib;

/*
 * Input's state is not abstract. It admits its state, and is therefore
 * controllable.
 */
type state = Hooks.State.t(string) => Hooks.nil;

type renderedTree;

type t = state => renderedTree;

let render:
  (~init: string=?, React.elem(React.empty)) =>
  React.renderable(t);
