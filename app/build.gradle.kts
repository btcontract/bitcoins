plugins {
    alias(libs.plugins.androidApplication)
}

apply(plugin = "com.soundcorset.scala-android")

android {
    namespace = "com.btcontract.wallet"
    compileSdk = 34

    defaultConfig {
        applicationId = "com.btcontract.wallet"
        versionName = "2.5.9"
        versionCode = 111
        targetSdk = 35
        minSdk = 28
    }

    buildTypes {
        release {
            isMinifyEnabled = false
        }

        debug {
            isMinifyEnabled = false
        }
    }

    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_1_8
        targetCompatibility = JavaVersion.VERSION_1_8
    }

    packaging {
        resources {
            excludes += "META-INF/**"
        }
    }
}

dependencies {
    compileOnly (
        fileTree(layout.buildDirectory) {
            include("**/R.jar")
        }
    )

    // Android-specific
    implementation(libs.material)
    implementation(libs.appcompat)
    implementation(libs.currencyedittext)
    implementation(libs.zxing.android.embedded)
    implementation(libs.multidex)
    implementation(libs.review)

    // Immortan
    implementation(libs.scala.library)
    implementation(libs.secp256k1.kmp.jni.android)
    implementation(libs.scala.parser.combinators.x.x1)
    implementation(libs.scodec.core.x.x1)
    implementation(libs.akka.actor.x.x1)
    implementation(libs.quicklens.x.x1)
    implementation(libs.rxscala.x.x1)

    implementation(libs.spray.json.x.x1)
    implementation(libs.json4s.native.x.x1)
    implementation(libs.bcprov.jdk15to18)
    implementation(libs.commons.codec)
    implementation(libs.netty.all)
    implementation(libs.okhttp)
    implementation(libs.guava)
}
