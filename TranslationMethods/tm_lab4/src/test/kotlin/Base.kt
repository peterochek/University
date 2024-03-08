import guru.nidi.graphviz.engine.Format
import guru.nidi.graphviz.engine.Graphviz
import guru.nidi.graphviz.model.Factory
import guru.nidi.graphviz.model.LinkSource
import java.nio.file.Paths

object Base {
    private val DATA_DIRECTORY = "src/main/resources"

    fun visualize(out: String, list: List<LinkSource>) {
        val outputFile = Paths.get(DATA_DIRECTORY).resolve("output").resolve("$out.png").toFile()

        val graph = Factory.graph("result").with(
            list.reversed()
        )

        Graphviz.fromGraph(graph.toMutable()).height(1200).render(Format.PNG).toFile(outputFile)
    }
}