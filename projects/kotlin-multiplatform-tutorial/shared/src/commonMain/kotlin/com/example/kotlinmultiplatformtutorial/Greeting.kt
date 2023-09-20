package com.example.kotlinmultiplatformtutorial

class Greeting {
    private val platform: Platform = getPlatform()

    fun greet(): String {
        return "Hello,\nGuess what it is: ${platform.name}!"
    }
}