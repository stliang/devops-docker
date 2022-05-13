# devops-terminal
DevOps infrastructure development local machine environment

## Build
```
nerdctl build --progress=plain -t devops .
```

## Run & Bind Local Directory
```
nerdctl run -v ~/Documents/github/data:/opt/app/data -v ~/Documents/github/devops-terminal:/opt/app/devops-terminal -it  devops:latest
```

## Test
```
gcloud compute instances list
```
