FROM continuumio/miniconda3 as stage_0

ARG CONDA_ENV=pscadf2c

RUN conda create -n $CONDA_ENV -c conda-forge f2c

RUN mkdir /portal

VOLUME /portal

COPY transpile.sh .

COPY p_code/modify_types.py .

COPY p_code/convert_fixed_format.py .

WORKDIR /portal

ENTRYPOINT [ "conda", "run", "--no-capture-output", "-n", "pscadf2c", \
             "/bin/bash", "/transpile.sh" ]
