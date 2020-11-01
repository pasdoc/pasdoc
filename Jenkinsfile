/* -*- mode: groovy -*-
  Confgure how to run our job in Jenkins.
  See https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins) .
  While PasDoc doesn't use Castle Game Engine,
  but it uses CGE Jenkins infrastructure on http://jenkins.castle-engine.io/ ,
  including a Docker image that contains various versions of FPC.
*/

pipeline {
  agent {
    docker {
      image 'kambi/castle-engine-cloud-builds-tools:cge-none'
    }
  }
  stages {
    stage('Test') {
      steps {
        sh 'source /usr/local/fpclazarus/bin/setup.sh default && make'
        // TODO: kambi/castle-engine-cloud-builds-tools Docker needs git to make this possible
        // sh 'source /usr/local/fpclazarus/bin/setup.sh default && make tests'
      }
    }
    stage('Build') {
      steps {
        sh 'www/snapshots/build.sh'
      }
    }
  }
  post {
    success {
      archiveArtifacts artifacts: 'pasdoc-*.tar.gz,pasdoc-*.zip'
    }
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
    /* TODO: In the future this should kick rebuild of
       kambi/castle-engine-cloud-builds-tools:cge-none
       since it may include latest PasDoc. */
  }
}
