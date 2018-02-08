

color=`tput setaf 48`
reset=`tput setaf 7`

echo
echo "${color}Copying source files to MiniLatex package${reset}"

cp src/MiniLatex/Accumulator.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Configuration.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Differ.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Driver.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/HasMath.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/KeyValueUtilities.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/LatexDiffer.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/LatexState.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Paragraph.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Parser.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/ParserTools.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Render.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/RenderToLaTex.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Image.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Html.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/JoinStrings.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/RenderLatexForExport.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/FastExportToLatex.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Utility.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/Source.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/StateReducerHelpers.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/ParserHelpers.elm ../MiniLatex/src/MiniLatex/
cp src/MiniLatex/ErrorMessages.elm ../MiniLatex/src/MiniLatex/




cp src/MiniLatex/Accumulator.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Configuration.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Differ.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Driver.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/HasMath.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/KeyValueUtilities.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/LatexDiffer.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/LatexState.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Paragraph.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Parser.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/ParserTools.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Render.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/RenderToLatex.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Image.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Html.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/JoinStrings.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/RenderLatexForExport.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/FastExportToLatex.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Source.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/Utility.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/StateReducerHelpers.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/ParserHelpers.elm ../MiniLatexTester/src/MiniLatex/
cp src/MiniLatex/ErrorMessages.elm ../MiniLatexTester/src/MiniLatex/
 



echo
echo "${color}Copying README to MiniLatex package${reset}"

cp src/MiniLatex/README.MD ../MiniLatex/
cp src/MiniLatex/README.MD ../MiniLatexTester/


echo
echo "${color}Copying tests files to MiniLatex package${reset}"


cp tests/AccumulatorTest.elm ../MiniLatex/tests/
cp tests/DifferTest.elm ../MiniLatex/tests/
cp tests/ParserTest.elm ../MiniLatex/tests/
cp tests/RenderTest.elm ../MiniLatex/tests/
cp tests/DriverTest.elm ../MiniLatex/tests/

cp tests/AccumulatorTest.elm ../MiniLatexTester/tests/
cp tests/DifferTest.elm ../MiniLatexTester/tests/
cp tests/ParserTest.elm ../MiniLatexTester/tests/
cp tests/RenderTest.elm ../MiniLatexTester/tests/
cp tests/DriverTest.elm ../MiniLatexTester/tests/

echo
echo "${color}Done${reset}"
