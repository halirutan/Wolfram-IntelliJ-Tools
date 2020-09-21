package de.halirutan.wlintellij

import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.Test

internal class TrieTest {

    private val examples = listOf(
        "Plot",
        "\$RecursionLimit",
        "AB\$CD\$",
        "$0123"
    )

    private val negativeExamples = listOf(
        "0Plot",
        "Plot_me"
    )

    @Test
    fun test_SymbolPattern() {
        val trie = Trie()
        for(e in examples) {
            Assertions.assertTrue(trie.isValidKey(e))
        }
        for(e in negativeExamples) {
            Assertions.assertFalse(trie.isValidKey(e))
        }
    }

}