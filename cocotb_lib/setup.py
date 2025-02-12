from setuptools import setup, find_packages

setup(
    name='cocotb_lib',
    version='1.0.0',
    packages=find_packages(exclude=["__pycache__"]),
    install_requires=[ "cocotb" ],
)
