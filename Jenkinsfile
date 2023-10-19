pipeline {
    agent any
    options {
        timestamps()
        ansiColor("xterm")
        disableConcurrentBuilds()
    }

    stages {
        stage('env') {
            steps {
                sh "env"
            }
        }
        stage('build') {
            steps {
                echo "no build yet"
            }
        }

        stage('lint') {
            steps {
                echo "no lint yet"
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
