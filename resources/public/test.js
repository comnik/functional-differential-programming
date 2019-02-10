function Test ({id}) {
  const [x, setX] = React.useState(0);

  console.log("re-rendering", id, x);

  return  React.createElement("button", {onClick: () => setX(x + 1)});
}

function Root () {
  console.log("re-rendering root");
  
  return React.createElement("div", {}, [
    React.createElement(Test, {id: 0, key: 0}),
    React.createElement(Test, {id: 1, key: 1}),
    React.createElement(Test, {id: 3, key: 2}),
  ])
}

ReactDOM.render(React.createElement(Root), document.getElementById("app-container"));
