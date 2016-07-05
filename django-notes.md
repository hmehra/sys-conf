# Django Notes -

### Creating vitual environment
    $ python3 -m venv myvenv
    $ virtualenv --python=python3.4 myvenv

### Activate venv
    $ source myvenv/bin/activate

### Install Django
   (myvenv) ~$ pip install django==1.9

### Init website
    (myvenv) ~/djangogirls$ django-admin startproject mysite .

### Create database
    (myvenv) ~/djangogirls$ python manage.py migrate

### Run webserver
    (myvenv) ~/djangogirls$ python manage.py runserver

### Init web app
    (myvenv) ~/djangogirls$ python manage.py startapp blog
    Add app to installed apps in settings.py in website

### Make database fields
    Make changes in models.py

## Make database table
    (myvenv) ~/djangogirls$ python manage.py makemigrations blog
    Register your model in web appp admin.py

## Create user for website
    (myvenv) ~/djangogirls$ python manage.py createsuperuser