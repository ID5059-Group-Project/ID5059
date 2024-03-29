.. image:: doc/_static/rpy2_logo_64x64.png

.. image:: https://img.shields.io/pypi/v/rpy2.svg?style=flat-square
    :target: https://pypi.python.org/pypi/rpy2

.. image:: https://travis-ci.com/rpy2/rpy2.svg?branch=master
    :target: https://travis-ci.com/rpy2/rpy2

.. image:: https://codecov.io/gh/rpy2/rpy2/branch/master/graph/badge.svg
  :target: https://codecov.io/gh/rpy2/rpy2

.. image:: https://github.com/rpy2/rpy2/workflows/Python%20package/badge.svg


Installation
============

`pip` should work out of the box:

    pip install rpy2

The package is known to compile on Linux, MacOSX, and Windows
(provided that developper tools are installed, and you are ready
figure out how by yourself).

In case you find yourself with this source without any idea
of what it takes to compile anything on your platform, try first

    python setup.py install

Non-system R installations
--------------------------

Whenever R is in not installed in a system location, the system might not
know where to find the R shared library.

If `R` is in the `PATH`, that is entering `R` on the command line successfully starts
an R terminal, but rpy2 does not work because of missing C libraries, try the following
before starting Python:


    export LD_LIBRARY_PATH="$(python -m rpy2.situation LD_LIBRARY_PATH)":${LD_LIBRARY_PATH}


Docker
------

**Note: the automated build of Docker image is currently broken.**

Alternatively, there is a Docker image available to try rpy2 out
without concerns about the installation process.

To run the ipython console:

    docker run -it --rm -p 8888:8888 rpy2/jupyter-ubuntu ipython

To run jupypter notebook on port 8888:

    docker run --rm -p 8888:8888 rpy2/jupyter-ubuntu

More information about Docker images can be found in the
`docker image repository <https://github.com/rpy2/rpy2-docker>`_.


Documentation
=============

Documentation is available either in the source tree (to be built),
or online (on readthedocs).

Testing
=======

`rpy2` is now relying on `pytest`, with the plugin `pytest-cov` for code coverage. To
test the package from the source tree, either to check and installation on your system
or before submitting a pull request, do:

    pytest tests/

For code coverage, do:

    pytest --cov=rpy2.rinterface_lib \
           --cov=rpy2.rinterface \
	   --cov=rpy2.ipython \
	   --cov=rpy2.robject \
	   tests

For more options, such as how to run specify tests, please refer to the `pytest`
documentation.


License
=======

RPy2 can be used under the terms of the GNU
General Public License Version 2 or later (see the file
gpl-2.0.txt). This is the very same license R itself is released under.
