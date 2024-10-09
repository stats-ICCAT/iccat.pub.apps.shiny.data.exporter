docker build -t iccat/apps/data_exporter:0.1.0 \
             --build-arg GITHUB_AUTH_TOKEN=$GITHUB_AUTH_TOKEN \
             --progress=plain \
             -f Dockerfile.arm .

