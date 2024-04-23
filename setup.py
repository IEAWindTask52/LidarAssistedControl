from setuptools import setup, find_packages


setup(
    name='LidarAssistedControl',
    url='https://github.com/IEAWindTask52/LidarAssistedControl',
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
