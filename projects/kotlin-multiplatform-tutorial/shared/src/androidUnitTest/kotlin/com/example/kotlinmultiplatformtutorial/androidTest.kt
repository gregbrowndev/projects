package com.example.kotlinmultiplatformtutorial

import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidGreetingTest {

    @Test
    fun testExample() {
        assertTrue("Check Android is mentioned", Greeting().greet().any { it.contains("Android") or it.contains("Android".reversed()) })
    }
}