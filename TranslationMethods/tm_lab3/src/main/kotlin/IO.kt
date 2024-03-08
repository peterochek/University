object IO {
    fun ioLine(ioCmd: String, arg: String, curTmpVar: TmpVar, indent: Int): String {
        return when (ioCmd) {
            "print" -> "printf(\"%lf\\n\", $arg);"
            "printarr" -> printArr(arg, curTmpVar, indent);
            "read" -> "scanf(\"%lf\", &$arg);"

            else -> throw IllegalArgumentException("No such built-in function")
        }
    }

    private fun printArr(arrName: String, curTmpVar: TmpVar, indent: Int) = buildString {
        val iterator = curTmpVar.createTmpVar()
        val arrayLen = curTmpVar.createTmpVar()
        appendLine("${"\t".repeat(indent - 1)}int $arrayLen = sizeof($arrName) / sizeof($arrName[0]);")
        appendLine("${"\t".repeat(indent)}for (int $iterator = 0; $iterator < $arrayLen; $iterator++) {")
        appendLine("${"\t".repeat(indent + 1)}printf(\"%lf\\n\", $arrName[$iterator]);")
        appendLine("${"\t".repeat(indent)}}")
    }
}
