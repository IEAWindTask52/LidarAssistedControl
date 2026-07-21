"""
Sets up an isolated Python environment for the LAC Summer Games 2026
"Ultra Marathon" and installs the packages listed in requirements.txt.

Usage:
    python setup_python.py

This creates a virtual environment in UltraMarathon/.venv (kept local to
this folder so it never touches your system/global Python) and installs
numpy, scipy, h5py, matplotlib and rainflow into it.

After it finishes, activate the environment and run the main script:
    Windows (PowerShell):  .venv\\Scripts\\Activate.ps1
    Windows (cmd):         .venv\\Scripts\\activate.bat
    macOS / Linux:         source .venv/bin/activate

    python RunUltraMarathon.py

Note: RunUltraMarathon.py loads its data/parameter files via paths that
are relative to the current working directory (e.g. 'functions/...',
'data/...'), so run it (or let your IDE run it) with UltraMarathon/ as
the working directory.

If you'd rather run RunUltraMarathon.py by clicking "Run" in an IDE, this
script also prints the venv's Python path at the end, plus where to paste
it in VS Code, PyCharm and Spyder.
"""

import os
import subprocess
import venv

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
VENV_DIR = os.path.join(THIS_DIR, '.venv')
REQUIREMENTS_FILE = os.path.join(THIS_DIR, 'requirements.txt')


def venv_python(venv_dir):
    if os.name == 'nt':
        return os.path.join(venv_dir, 'Scripts', 'python.exe')
    return os.path.join(venv_dir, 'bin', 'python')


def main():

    if not os.path.isdir(VENV_DIR):
        print(f'Creating virtual environment in {VENV_DIR} ...')
        venv.EnvBuilder(with_pip=True).create(VENV_DIR)
    else:
        print(f'Virtual environment already exists in {VENV_DIR}, reusing it.')

    python_exe = venv_python(VENV_DIR)

    print('Upgrading pip ...')
    subprocess.run([python_exe, '-m', 'pip', 'install', '--upgrade', 'pip'], check=True)

    print(f'Installing packages from {REQUIREMENTS_FILE} ...')
    subprocess.run([python_exe, '-m', 'pip', 'install', '-r', REQUIREMENTS_FILE], check=True)

    print()
    print('Setup complete. Activate the environment with:')
    if os.name == 'nt':
        print(r'  .venv\Scripts\Activate.ps1   (PowerShell)')
        print(r'  .venv\Scripts\activate.bat    (cmd)')
    else:
        print('  source .venv/bin/activate')
    print()
    print('Then run:')
    print('  python RunUltraMarathon.py')
    print()
    print('Using an IDE instead? Point its Python interpreter at:')
    print(f'  {python_exe}')
    print('  VS Code:  Ctrl+Shift+P -> "Python: Select Interpreter" -> paste the path above')
    print('  PyCharm:  Settings -> Project -> Python Interpreter -> Add -> Existing -> paste the path above')
    print('  Spyder:   Tools -> Preferences -> Python interpreter -> Use the following interpreter -> paste the path above')


if __name__ == '__main__':
    main()
