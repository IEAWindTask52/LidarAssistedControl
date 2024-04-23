from setuptools import setup, find_packages


setup(
    name='LidarAssistedControl',
    url='https://github.com/IEAWindTask52/LidarAssistedControl',
    author='Feng Guo,David Schlipf, Aravind v, Simon Weich',
    python_requires=">=3.10",
    packages=find_packages(),
    install_requires=[
        "matplotlib",
        "numpy",
        "pandas",
        "scipy",
        "joblib"
    ]
)
