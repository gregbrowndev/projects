from django.db import models

# Create your models here.
class Foo(models.Model):
    bar = models.IntegerField()
    baz = models.IntegerField()

    class Meta:
        db_constraints = {
            'bar_equal_baz': 'check (bar = baz)',
        }

