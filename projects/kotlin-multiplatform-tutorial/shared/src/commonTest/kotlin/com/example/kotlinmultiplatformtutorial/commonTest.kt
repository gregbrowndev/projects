package com.example.kotlinmultiplatformtutorial

import kotlin.test.Test
import kotlin.test.assertTrue

class CommonGreetingTest {

    @Test
    fun testExample() {
        assertTrue(Greeting().greet().any { it.contains("Hello") or it.contains("Hi")}, "Check 'Hello' is mentioned")
    }
}