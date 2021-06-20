# Django + Celery + Docker

## Introduction

We can use Celery to run tasks outside of Django's request-response cycle, such as from sending emails, [scraping a website](https://www.codementor.io/johnnyb/how-to-write-a-web-scraper-in-nodejs-du108266t) or running periodic tasks. 

Might be useful to look into:

* [Celery Retry](http://docs.celeryproject.org/en/latest/userguide/tasks.html#retrying) for re-executing failed tasks
* [Exponential backoff algorithm](https://en.wikipedia.org/wiki/Exponential_backoff) for limiting the task schedule when the task is failing
* [Demonisation](http://docs.celeryproject.org/en/stable/userguide/daemonizing.html) for running Celery in production

Task management best practices:

* It's good practice to keep unreliable and time-consuming tasks outside the request time.
* Long-running tasks should be executed in the background by worker processes (or other paradigms).
* Background tasks can be used for various tasks that are not critical for the basic functioning of the application.
* Celery can also handle periodic tasks using the celery beat service.
Tasks can be more reliable if made idempotent and retried (maybe using exponential backoff).

## Setup

### Docker 

This project now uses Docker so no setup is required. See the docker-compose.yml file.

Still to do:

* The Celery processes should not be running as root. See the [USER](https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/#user) docs.
* Add [Celery Flower](https://github.com/crgwbr/docker-celery-flower) for celery task monitoring.


### Native

Run `redis-server`:

```bash
redis-server
```

Run Celery workers:

```bash
celery worker -A celeryapp --loglevel=debug --concurrency=4
```
where `celeryapp` is the name of the celery app specified in `celeryapp/celery.py`.

Run beat for periodic tasks:

```tasks
celery -A quick_publisher beat
```

Note - when developing, the celery worker process must be restarted to register new tasks. Otherwise, the task will fail.

## Useage

### Tasks

To test the basic workings of the celery-enabled application we can call our task from the console.

Example:

```console
from main.tasks import add, mul, xsum
result = mul.delay(40,56565)
result.ready()

>>> True

result.get()

>>> 2262600
``` 

This demonstrates the celery app has found the tasks in the `main.tasks` module and can execute them correctly. 

### Periodic Tasks

Most mature web applications send their users lifecycle emails in order to keep them engaged. Some common examples of lifecycle emails:

* monthly reports
* activity notifications (likes, friendship requests, etc.)
* reminders to accomplish certain actions, such as email verification

[See the docs](http://docs.celeryproject.org/en/latest/userguide/periodic-tasks.html). 

`celery beat` is a scheduler that kicks off tasks at regular intervals which are then executed by available worker nodes in the cluster.

> You have to ensure only a single scheduler is running for a schedule at a time, otherwise you’d end up with duplicate tasks. Using a centralized approach means the schedule doesn’t have to be synchronized, and the service can operate without using locks.

The default scheduler stores the schedule in the `celerybeat-schedule` file. There are [other schedulers](http://docs.celeryproject.org/en/latest/userguide/periodic-tasks.html#beat-custom-schedulers), such as the one provided by [django-celery-beat](django_celery_beat) that store the schedule in the database and provide a nice admin interface to manage periodic tasks at runtime.

#### Creating Periodic Tasks

There seems to be some dispute about where to place the periodic task code, whether to manage them in a centralised place (i.e. within the top-level celery.py module), or whether to define the periodic tasks closer to where the task logic is (i.e. in `tasks.py`), e.g. [here](https://stackoverflow.com/questions/41697364/adding-periodic-tasks-in-celery-django-inside-an-app). 

##### Example - periodic tasks defined directly in beat schedule setting:

1. In `settings.py`:
```python
CELERY_BEAT_SCHEDULE = {
    'add_task_30s': {
        'task': 'main.tasks.add',
        'schedule':  30.0,
        'args': (2, 2)
    }
}
```

This would allow tasks to targetted at specific platforms, i.e. testing, production, etc. However, this is likely an advanced usecase.

2. In top-level `celery.py`:
```python
# After setting up celery and doing autodiscover_tasks ...

app.conf.beat_schedule = {
    'add_task_30s': {
        'task': 'main.tasks.add',
        'schedule':  30.0,
        'args': (2, 2)
    }
}
```

This could be used to define tasks common to all platforms. Would override the existing dictionary unless you used `app.conf.beat_schedule.update({...})`. You could do the same thing with `base.py` settings and extending the schedule in `dev.py`, etc.

##### Example - adding periodic tasks using `add_periodic_task`

This method is likely preferred as periodic tasks can be added without worrying about overriding existing tasks. In addition, it is possible to define the periodic tasks within the app, which IMO is more encapsulated with the business logic of the app.  

1. In the top-level `celery.py`:

```python
# After setting up celery and doing autodiscover_tasks ...

@app.on_after_configure.connect
def setup_periodic_tasks(sender, **kwargs):

    sender.add_periodic_task(
        30.0,
        signature('main.tasks.add', args=(3,7)),
        name='add_task_30s'
    )
```

2. In `main/tasks.py`:

I think it should be possible, e.g. with:
```python
from celery import current_app

app = current_app()

@app.on_after_configure.connect
def setup_periodic_tasks(sender, **kwargs):
    ...
```

However, this doesn't quite seem to work properly, `on_after_configure` might not be the correct decorator in this case. It would be interesting to find a solution for this in future.

### Running in Production

Using the `celery worker` management command, i.e.

```console
celery -A celeryapp worker -l info
``` 

is not recommended in production setting. Instead, consult [Daemonization](http://docs.celeryproject.org/en/latest/userguide/daemonizing.html#daemonizing).


### Django Celery Beat Extension

With django-celery-beat installed and added to `INSTALLED_APPS`:
```python
pip install django-celery-beat
```

After running `manage migrate`, an admin interface will be deployed allowing periodic tasks schedule interacted with at runtime as it seems better to encapsulate periodic tasks with their corresponding business logic. 

Simply provide the custom scheduler when you run `beat` so the schedule is stored in the database:
```bash
celery -A celeryapp beat  -l info --scheduler django_celery_beat.schedulers:DatabaseScheduler
```

### Django Celery Results Extension 

The [django-celery-results](https://django-celery-results.readthedocs.io/en/latest/) library allows you to store Celery task results using the Django ORM. The results are stored in `TaskResults`, which can be queried.

## Further Notes

#### Tasks using Signals

Tasks are often fired off by writing custom signal functions. Example:

```python
from django.db.models import signals
from main.tasks import mytask
 
def mymodel_post_save(sender, instance, signal, *args, **kwargs):
    if not instance.is_verified:
        # Send verification email
        mytask.delay(instance.pk)
 
signals.post_save.connect(mymodel_post_save, sender=MyModel)
```

Here, we are attaching the method `mymodel_post_save` to `post_save` signal of `MyModel`. The method triggers the `mytask` task.

#### Avoiding Issues with Relative Imports

We use `from __future__ import absolute_import` (which actually only applies if running in a python 2 environment), to ensure there is no mismatch between the task names generated by the celery work and celery app. In python 3 this is the default apparently.


#### Updating the State of a Task

Using `current_task.update_state()` method, we can pass the status of the task completed to the message broker every 30 iterations. Example:

```python
from celery import shared_task,current_task
from numpy import random
from scipy.fftpack import fft
    
@shared_task
def fft_random(n):
    for i in range(n):
        x = random.normal(0, 0.1, 2000)
        y = fft(x)
        if(i%30 == 0):
            process_percent = int(100 * float(i) / float(n))
            current_task.update_state(state='PROGRESS',
                meta={'process_percent': process_percent})
    return random.random()
```