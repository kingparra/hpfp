# Configuration file for the Sphinx documentation builder
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# Project information
project   = 'hpfp'
copyright = '2020, Chris King-Parra'
author    = 'Chris King-Parra'

# General configuration
extensions = [ ]
templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']
master_doc = 'index'

# Options for HTML output
html_theme = 'alabaster'
html_static_path = ['_static']
