/* -*- mode: groovy -*-
  Confgure how to run our job in Jenkins.
  See https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins) .
  While PasDoc doesn't use Castle Game Engine,
  but it uses CGE Jenkins infrastructure on http://jenkins.castle-engine.io/ ,
  including a Docker image that contains various versions of FPC.
*/

pipeline {
  agent any
  stages {
    stage('Test') {
      agent {
        /* We need to run this on a host where "git" is installed
           (used by "make tests"). */
        label 'web-michalis-ii-uni-wroc-pl'
        /*
        docker {
          image 'kambi/castle-engine-cloud-builds-tools:cge-none'
        }
        */
      }
      steps {
        sh 'source /usr/local/fpclazarus/bin/setup.sh default && make'
        sh 'source /usr/local/fpclazarus/bin/setup.sh default && make tests'
      }
    }
    stage('Build') {
      agent {
        docker {
          image 'kambi/castle-engine-cloud-builds-tools:cge-none'
        }
      }
      steps {
        sh 'www/snapshots/build.sh'
        stash name: 'snapshots-to-publish', includes: 'pasdoc-*.tar.gz,pasdoc-*.zip'
        /* Do not defer "archiveArtifacts" to later (like post section),
           as this command must run in the same agent and Docker container
           as build.sh. */
        archiveArtifacts artifacts: 'pasdoc-*.tar.gz,pasdoc-*.zip'
      }
    }
    stage('Upload Snapshots') {
      /* This must run on michalis.ii.uni.wroc.pl, outside Docker,
         since it directly copies the files. */
      agent { label 'web-michalis-ii-uni-wroc-pl' }
      when { branch 'master' } /* upload only the "master" branch results */
      steps {
        unstash name: 'snapshots-to-publish'
        sh 'www/snapshots/upload.sh'
      }
    }
  }
  post {
    regression {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build is again successfull: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    /* TODO: In the future this should kick rebuild of
       kambi/castle-engine-cloud-builds-tools:cge-none
       since it may include latest PasDoc. */
  }
}
