import abc
from sympy import latex
from typing import List


class Renderable(abc.ABC):
  @abc.abstractmethod
  def render(self) -> str:
    pass


class Expression(Renderable):
  def __init__(self, expression, prefix='', suffix=''):
    self.prefix = prefix
    self.suffix = suffix
    self.expression = expression

  def render(self) -> str:
    ''.join([self.prefix, latex(self.expression), self.suffix])


class Container(Renderable):
  def __init__(self, contents: Renderable, begin: str, end: str):
    self.contents = contents
    self.begin = begin
    self.end = end

  def render(self) -> str:
    r = [self.begin]
    r.extend(r.render() for r in self.contents)
    r.append(self.end)
    return '\n'.join(dropnil(r))
  

def Equation(**kwargs):
  return Container(
    begin='\\begin{equation}', 
    end='\\end{equation}', 
    **kwargs
  )


def Align(**kwargs):
  return Container(
    begin='\\begin{align}', 
    end='\\end{align}', 
    **kwargs
  )


  



def render_equation(expression, label=None, **kwargs):
  return '\n'.join(dropnil([
      '\\begin{equation}',
      render_expression(expression, **kwargs),
      render_label(label),
      '\\end{equation}'
  ]))


def render_array(*expressions, label=None, **kwargs):
  return '\n'.join(dropnil([
      '\\begin{align}'
  ] + [
      render_expression(e, **kwargs)
      for e in expressions
  ] + [
      render_label(label),
      '\\end{align}'
  ]))


def render_label(label=None):
  return ('(\\#eq:%s)' % label) if label is not None else ''


def render_expression(expression, prefix='', suffix=''):
  return ''.join([prefix, latex(expression), suffix])


def render_md(latex):
  return '\n'.join(['$$', latex, '$$'])


def dropnil(lst):
  return [x for x in lst if x]


def rmd(*expressions, **kwargs):
  return render_md(
    (
      render_equation if len(expressions) == 1
      else render_array
    )(*expressions, **kwargs)
  )


def rmdp(*args, **kwargs):
  print(rmd(*args, **kwargs))
