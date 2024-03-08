package util

import guru.nidi.graphviz.model.Factory
import guru.nidi.graphviz.model.LinkSource
import util.ID.inc

class GraphVizStruct {
    val list: MutableList<LinkSource> = ArrayList()

    fun add(parent: String, child: String) {
       add(Factory.node(parent).link(Factory.node(inc(child))))
    }

    private fun add(element: LinkSource) = list.add(element)
}