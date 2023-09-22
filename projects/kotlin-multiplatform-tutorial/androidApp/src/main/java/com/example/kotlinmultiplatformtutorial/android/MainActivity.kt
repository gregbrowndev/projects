package com.example.kotlinmultiplatformtutorial.android

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.Divider
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import com.example.kotlinmultiplatformtutorial.Greeting

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MyApplicationTheme {
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = MaterialTheme.colors.background
                ) {
                    var phrases by remember { mutableStateOf(listOf("Loading...")) }
                    LaunchedEffect(true) {
                        phrases = try {
                            Greeting().greet()
                        } catch (e: Exception) {
                            listOf(e.localizedMessage ?: "error")
                        }
                    }
                    GreetingView(phrases)
                }
            }
        }
    }

    @Composable
    fun GreetingView(phrases: List<String>) {
        LazyColumn(
            contentPadding = PaddingValues(20.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp)
        ) {
            items(phrases) { phrase ->
                Text(phrase)
                Divider()
            }
        }
    }

    @Preview
    @Composable
    fun DefaultPreview() {
        MyApplicationTheme {
            GreetingView(listOf("Hello", "Android!"))
        }
    }
}