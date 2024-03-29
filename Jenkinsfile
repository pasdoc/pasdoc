/* -*- mode: groovy -*-

  Configure how to run our job in Jenkins.
  See https://castle-engine.io/cloud_builds_jenkins .
  While PasDoc doesn't use Castle Game Engine,
  but it uses CGE Jenkins infrastructure on https://jenkins.castle-engine.io/ ,
  including a Docker image that contains various versions of FPC.

  The resulting binaries are available on
  https://pasdoc.github.io/DevelopmentSnapshots .
*/

pipeline {
  agent none /* each stage has different agent */
  stages {
    /* Build for each platform in parallel.
       See https://stackoverflow.com/questions/43913698/jenkinsfile-parallel-directive
       https://www.jenkins.io/blog/2017/09/25/declarative-1/
       for parallel syntax. */
    stage('Run parallel builds') {
      parallel {
        stage('Linux stage') {
          agent {
            docker {
              image 'kambi/castle-engine-cloud-builds-tools:cge-none'
            }
          }
          stages {
            stage('Test') {
              steps {
                sh 'source /usr/local/fpclazarus/bin/setup.sh default && make'
                sh 'source /usr/local/fpclazarus/bin/setup.sh default && make tests'
              }
            }
            stage('Clean') {
              steps {
                sh 'rm -f pasdoc-*.tar.gz pasdoc-*.zip'
                sh 'make clean'
              }
            }
            /* We don't need to package sources anymore.
               GIT tag is enough, and GitHub makes a zip from it anyway.
            stage('Package sources') {
              steps {
                sh 'make dist-src'
              }
            }
            */
            stage('Build Linux/x86_64') {
              steps {
                sh 'make dist-linux-x86_64'
              }
            }
            stage('Build Windows/i386') {
              steps {
                sh 'make dist-win32'
              }
            }
            stage('Build Windows/x86_64') {
              steps {
                sh 'make dist-win64'
              }
            }
            stage('Archiving') {
              steps {
                archiveArtifacts artifacts: 'pasdoc-*.tar.gz,pasdoc-*.zip'
              }
            }
          }
        }
        stage('Raspberry Pi') {
          agent {
            label 'raspberry-pi-cge-builder'
          }
          stages {
            stage('Clean RaspberryPi') {
              steps {
                sh 'rm -f pasdoc-*.tar.gz pasdoc-*.zip'
                sh 'make clean'
              }
            }
            stage('Test RaspberryPi') {
              steps {
                sh 'make'
                sh 'make tests'
              }
            }
            stage('Build RaspberryPi') {
              steps {
                sh 'make dist-linux-arm'
              }
            }
            stage('Archiving RaspberryPi') {
              steps {
                archiveArtifacts artifacts: 'pasdoc-*.tar.gz,pasdoc-*.zip'
              }
            }
          }
        }
        stage('macOS') {
          agent {
            label 'mac-cge-builder'
          }
          stages {
            stage('Clean macOS') {
              steps {
                sh 'rm -f pasdoc-*.tar.gz pasdoc-*.zip'
                sh 'make clean'
              }
            }
            stage('Test macOS') {
              steps {
                sh 'make'
                sh 'make tests'
              }
            }
            stage('Build macOS') {
              steps {
                sh 'make dist-darwin-x86_64'
              }
            }
            stage('Archiving macOS') {
              steps {
                archiveArtifacts artifacts: 'pasdoc-*.tar.gz,pasdoc-*.zip'
              }
            }
          }
        }
      }
    }
  }
  post {
    regression {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build is again successfull: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
