import Constants.FILE_EXTENSION
import org.antlr.v4.runtime.CharStream
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

private fun translate(stream: CharStream): TranslatedResult {
    val errorListener = ErrorListener()
    val lexer = CobraLexer(stream).apply {
        removeErrorListeners()
        addErrorListener(errorListener)
    }

    val parser = CobraParser(CommonTokenStream(lexer)).apply {
        removeErrorListeners()
        addErrorListener(errorListener)
    }

    val translator = CobraTranslator()

    return try {
        val result = translator.visit(parser.script())
        if (errorListener.isEmpty()) {
            TranslatedResult.TranslatedOk(result)
        } else {
            TranslatedResult.TranslatedErr(errorListener.getErrors)
        }
    } catch (error: Exception) {
        error.message?.let { errorListener.addError(it) }
        TranslatedResult.TranslatedErr(errorListener.getErrors)
    }
}

fun getFilesWithExtension(directoryPath: String, extension: String): List<File> {
    val directory = File(directoryPath)
    if (!directory.exists() || !directory.isDirectory) {
        throw IllegalArgumentException("Invalid directory path: $directoryPath")
    }

    return directory.listFiles { file -> file.isFile && file.extension == extension }?.toList() ?: emptyList()
}

fun processFile(fileName: String) {
    val extIdx = fileName.indexOf(FILE_EXTENSION)
    if (!fileName.endsWith(FILE_EXTENSION) || extIdx == -1) {
        System.err.println("$fileName not an $FILE_EXTENSION file")
        return
    }

    val translated = translate(CharStreams.fromFileName(fileName))

    if (translated is TranslatedResult.TranslatedOk) {
        val cFileName = "${fileName.removeSuffix(FILE_EXTENSION)}c"
        Files.newBufferedWriter(
            Paths.get(cFileName)
        ).use { it.write(translated.translated) }
    } else if (translated is TranslatedResult.TranslatedErr) {
        val prefix = "Errors occurred:\n"
        System.err.println(translated.errors.joinToString(prefix = prefix, separator = "\n"))
    }
}

fun compileWithGCC(file: File): Int {
    val fileName = "${file.absolutePath.removeSuffix(FILE_EXTENSION)}c"

    val processBuilder = ProcessBuilder("gcc", "-O3", fileName, "-o", fileName.removeSuffix(".c"))
    val process = processBuilder.start()
    process.waitFor()
    return process.exitValue()
}


fun main(args: Array<String>) {
    if (args.isEmpty()) {
        System.err.println("Enter directory with `${FILE_EXTENSION}` files")
        return
    }

    val extension = FILE_EXTENSION

    val filesWithExtension = getFilesWithExtension(args[0], extension)

    filesWithExtension.onEach { processFile(it.absolutePath) }
        .forEach { compileWithGCC(it) }
}


sealed class TranslatedResult {
    class TranslatedOk(val translated: String) : TranslatedResult()

    class TranslatedErr(val errors: List<String>) : TranslatedResult()
}