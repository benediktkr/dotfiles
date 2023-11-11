pipeline {
    agent any
    options {
        timestamps()
        ansiColor("xterm-256color")
        disableConcurrentBuilds()
        buildDiscarder(logRotator(daysToKeepStr: '30', numToKeepStr: '10', artifactNumToKeepStr: '1'))
    }
    stages {
        stage('version') {
            steps {
                script {
                    env.VERSION = sh(script: "date -I", returnStdout: true).trim().replace("-",".")
                    currentBuild.description = env.VERSION
                }
            }
        }
    }
    post {
        cleanup {
            cleanWs(deleteDirs: true,
                    disableDeferredWipeout: true,
                    notFailBuild: true)
        }
    }
}
