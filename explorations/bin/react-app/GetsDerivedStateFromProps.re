open ReactLib;

let render = (~txt="deafult", ~size, children) =>
  React.Stateful(
    hooks => {
      let ((curSize, curChangeCount), setState, hooks) =
        Hooks.state((size, 0), hooks);
      let nextChangeCount =
        curSize !== size ? curChangeCount + 1 : curChangeCount;
      (
        hooks,
        <>
          <Button txt="ButtonInContainerThatCountsSizeChanges" size=0>
            children
          </Button>
          <Div
            className={
              "size changed times:" ++ string_of_int(nextChangeCount)
            }
          />
        </>,
      );
    },
  );
