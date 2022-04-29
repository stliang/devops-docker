# devops-docker
Foundational infrastructure development local machine environment

## Build and Run Docker
```
docker build --progress=plain -t devops .
docker run -it  devops:latest
```

## Bind Local Directory
```
docker run -v ~/Documents/github/samples:/opt/app/data -it  devops:latest
```

## Test
```
gcloud compute instances list
```
