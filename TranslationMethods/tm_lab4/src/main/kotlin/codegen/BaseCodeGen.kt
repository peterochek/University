package codegen

import java.io.File
import java.nio.file.Paths
import kotlin.io.path.createDirectories
import kotlin.io.path.notExists

interface BaseCodeGen {
    val grammarName: String
    val fileName: String
    val path: String
    fun generateInner(): String

    fun codeGen() {
        val curGenFileName = Paths.get("src/main/kotlin/$path").resolve("$fileName.kt")

        if (curGenFileName.parent.notExists()) {
            curGenFileName.parent.createDirectories()
        }

        val file = File(curGenFileName.toString())

        file.createNewFile()

        val fileScript = buildString {
            if (file.parent != null) {
                appendLine("package $path")
                appendLine()
            }

            append(generateInner())
        }

        file.bufferedWriter().use { out ->
            out.write(fileScript)
        }
    }

    fun StringBuilder.append(line: String, tabsCount: Int = 1) {
        this.append("${TAB.repeat(tabsCount)}$line$NL")
    }

//    fun StringBuilder.appendLine(line: String, tabsCount: Int = 1) {
//        this.append("${TAB.repeat(tabsCount)}$line$NL")
//    }

    companion object {
        const val NL = "\n"
        private const val TAB = "    "
    }
}