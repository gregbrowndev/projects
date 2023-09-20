package com.example.kotlinmultiplatformtutorial

import kotlin.random.Random

class Greeting {
    private val platform: Platform = getPlatform()

    fun greet(): List<String> = buildList {
        add(if (Random.nextBoolean()) "Hi!" else "Hello!")
        add("Guess what it is: ${platform.name.reversed()}!")
        add("There are only ${daysUntilNewYear()} days until New Year! \uD83C\uDF86")
    }
}