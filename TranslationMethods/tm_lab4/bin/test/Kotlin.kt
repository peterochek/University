import Base.visualize
import kotlinDecl.KotlinDeclParser
import org.junit.jupiter.api.Test

class Kotlin {
    @Test
    fun singleDecl() {
        val input = "val aaa: Int = 5;"
        val res = KotlinDeclParser(input).parse().res

        visualize("SingleDecl", res!!.list)
    }

    @Test
    fun multipleDecl() {
        val input = "var b: Int = 6; var a: Int = 5;"
        val res = KotlinDeclParser(input).parse().res

        visualize("MultipleDecl", res!!.list)
    }
}