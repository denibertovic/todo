// :core — pure Kotlin/JVM module. No Android dependencies. This is
// where the wire-format types, parser, renderer, and CRDT live; it's
// JVM-testable without an emulator and shares zero code with Android
// SDK APIs so the same logic can be exercised from unit tests.
plugins {
    alias(libs.plugins.kotlin.jvm)
    alias(libs.plugins.kotlin.serialization)
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(17))
    }
}

kotlin {
    jvmToolchain(17)
}

dependencies {
    implementation(libs.kotlinx.serialization.json)
    implementation(libs.kotlinx.datetime)

    testImplementation(libs.kotest.runner.junit5)
    testImplementation(libs.kotest.assertions.core)
    testImplementation(libs.kotest.property)
}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
}
