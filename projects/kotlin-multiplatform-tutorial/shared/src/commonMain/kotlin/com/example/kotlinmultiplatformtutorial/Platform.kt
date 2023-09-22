package com.example.kotlinmultiplatformtutorial

interface Platform {
    val name: String
}

expect fun getPlatform(): Platform