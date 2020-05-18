import datetime
import re

from flask import request, redirect, url_for, render_template, Blueprint, flash, Markup
from peewee import *
from wtfpeewee.orm import model_form

from flaskext.rest import RestResource
from flaskext.utils import get_object_or_404, object_list

from api import api
from app import app, db
from auth import auth


wikify_re = re.compile(r'\b(([A-Z]+[a-z]+){2,})\b')

@app.template_filter('wikify')
def wikify(s):
    return Markup(wikify_re.sub(r'<a href="/wiki/\1/">\1</a>', s))

class WikiArticle(db.Model):
    name = CharField()
    content = TextField()
    modified_date = DateTimeField()

    class Meta:
        ordering = (('modified_date', 'desc'),)

    def __unicode__(self):
        return self.name

    def save(self):
        self.modified_date = datetime.datetime.now()
        return super(WikiArticle, self).save()


wiki = Blueprint('wiki', __name__, template_folder='templates')

@wiki.route('/')
@auth.login_required
def index():
    qr = WikiArticle.select()
    return object_list('wiki/index.html', qr)

@wiki.route('/<name>/', methods=['GET', 'POST'])
@auth.login_required
def detail(name):
    WikiForm = model_form(WikiArticle, only=('name', 'content',))

    try:
        article = WikiArticle.get(name=name)
    except WikiArticle.DoesNotExist:
        article = WikiArticle(name=name)

    if request.method == 'POST':
        form = WikiForm(request.form, obj=article)
        if form.validate():
            form.populate_obj(article)
            article.save()
            flash('Your changes have been saved')
            return redirect(url_for('wiki.detail', name=article.name))
        else:
            flash('There were errors with your submission')
    else:
        form = WikiForm(obj=article)

    return render_template('wiki/detail.html', article=article, form=form)

@wiki.route('/<name>/delete/', methods=['GET', 'POST'])
@auth.login_required
def delete(name):
    article = get_object_or_404(WikiArticle, name=name)
    if request.method == 'POST':
        article.delete_instance()
        return redirect(url_for('wiki.index'))

    return render_template('wiki/delete.html', article=article)