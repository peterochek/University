import codegen.CodeGen

const val DATA_PATH = "src/main/resources"

fun main() {
    CodeGen(DATA_PATH, "calcDouble", "CalcDouble.txt").codeGen()
    CodeGen(DATA_PATH, "calcFloat", "CalcFloat.txt").codeGen()
    CodeGen(DATA_PATH, "calcInt", "CalcInt.txt").codeGen()
    CodeGen(DATA_PATH, "calcLong", "CalcLong.txt").codeGen()
    CodeGen(DATA_PATH, "kotlinDecl", "KotlinDecl.txt").codeGen()
}