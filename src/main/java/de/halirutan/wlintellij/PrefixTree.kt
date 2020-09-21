@file:JvmName("Utils")

package de.halirutan.wlintellij

import java.lang.RuntimeException
import java.lang.StringBuilder

fun main() {
    val str = createReducedRegex(listOf("Hello", "Pine"))
    println(str)
}

fun createReducedRegex(words: Array<String>): String {
    return createReducedRegex(words.toList())
}

fun createReducedRegex(words: List<String>): String {
    val trie = Trie(words)
    val builder = StringBuilder()

    fun visit(node: Node) {
        builder.append(node.content)
        if (node.hasChildren()) {
            val keys = node.children.keys.sorted()
            if (keys.size == 1) {
                if (node.isWord) {
                    builder.append("(")
                    visit(node.children[keys[0]]!!)
                    builder.append(")?")
                } else {
                    visit(node.children[keys[0]]!!)
                }
            } else {
                builder.append("(")
                keys.forEachIndexed { id, c ->
                    visit(node.children[c]!!)
                    if (id < keys.size - 1) {
                        builder.append("|")
                    }
                }
                builder.append(")")
                if (node.isWord) {
                    builder.append("?")
                }
            }
        }
    }
    visit(trie.root)
    return builder.toString().replace("$","\\$").trim()
}


class Trie() {
    val root: Node = Node(' ')

    constructor(words: List<String>) : this() {
        words.forEach(::insert)
    }

    fun insert(word: String) {
        if (contains(word) || !isValidKey(word)) return
        var current = root
        word.forEach { char ->
            current = current.getOrCreateChild(char)
        }
        current.isWord = true
    }

    fun contains(word: String): Boolean {
        var current = root
        word.forEach { char ->
            if (current.hasChild(char)) {
                current = current.children[char] ?: throw RuntimeException("Key $char not a child")
            } else {
                return false
            }
        }
        return current.isWord
    }

    fun isValidKey(word: String): Boolean {
        return word.isNotBlank() && word.matches(Regex("(?:\\$|[a-zA-Z])[a-zA-Z0-9\$]*"))
    }
}


class Node(val content: Char) {
    val children: MutableMap<Char, Node> = hashMapOf()
    var isWord = false

    fun hasChild(char: Char): Boolean {
        return children.containsKey(char)
    }

    fun hasChildren(): Boolean {
        return !children.isNullOrEmpty()
    }

    fun getOrCreateChild(char: Char): Node {
        return if (children.containsKey(char)) {
            children[char]!!
        } else {
            children[char] = Node(char)
            children[char]!!
        }

    }
}

