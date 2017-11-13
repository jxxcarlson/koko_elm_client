

color=`tput setaf 48`
reset=`tput setaf 7`

echo
echo "${color}Copying source files to MiniLatex package${reset}"

cp src/MiniLatex/Accumulator.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/Configuration.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/Differ.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/Driver.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/KeyValueUtilities.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/LatexDiffer.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/Accumulator.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/LatexState.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/Parser.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/ParserTools.elm ../MiniLatex/src/MiniLatex
cp src/MiniLatex/Render.elm ../MiniLatex/src/MiniLatex


echo
echo "${color}Copying README to MiniLatex package${reset}"

cp src/MiniLatex/README.MD ../MiniLatex/


echo
echo "${color}Copying tests files to MiniLatex package${reset}"


cp tests/AccumulatorTest.elm ../MiniLatex/tests/
cp tests/DifferTest.elm ../MiniLatex/tests/
cp tests/ParserTest.elm ../MiniLatex/tests/
cp tests/RenderTest.elm ../MiniLatex/tests/

echo
echo "${color}Done${reset}"
