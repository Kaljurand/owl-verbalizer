plugins {
    java
}

group = "ch.uzh.ifi.attempto.owl"
version = "20110612"

java {
    sourceCompatibility = JavaVersion.VERSION_1_8
    targetCompatibility = JavaVersion.VERSION_1_8
}

dependencies {
    implementation("commons-logging:commons-logging:1.1")
    implementation("org.apache.httpcomponents:httpclient:4.1.1")
    implementation("org.apache.httpcomponents:httpcore:4.1.1")

    testImplementation("junit:junit:4.5")
}

repositories {
    mavenCentral()
}


sourceSets {
    main {
        java {
            srcDir("src")
        }
    }

    test {
        java {
            srcDir("test")
        }
    }
}

