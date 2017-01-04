# Start Jupyter in Docker on Mac

```bash
$ brew cask install docker
# Run Applications -> Docker
$ docker run -it --rm -p 8888:8888 -v `pwd`:/home/jovyan/work jupyter/tensorflow-notebook
```

