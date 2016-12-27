const fs = require('fs');
const child_process = require('child_process')

function compileAndImportDefinitons() {
  console.log('compiling level definitions');
  const output = child_process.execFileSync(
    'elm-make',
    ["src/Level/Definitions.elm", "--output", "_build/definitions.js"],
    {encoding: 'utf8'});
  return require('../_build/definitions')
};

Elm = compileAndImportDefinitons();
app = Elm.Level.Definitions.worker()

var currentLevel = 0;

app.ports.writeCode.subscribe((fromElm) => {
  fs.writeFile("src/Level/Generated.elm", fromElm, function(err) {
    if(err) {
      return console.log(err);
    }
    console.log('written level');

    currentLevel = currentLevel + 1;
    callDefinitions(currentLevel);
  });
});

function callDefinitions(n) {
  console.log('generating level ' + n);
  app.ports.generateLevel.send(n);
};

callDefinitions(currentLevel);
