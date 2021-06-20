import importlib

if __name__=="__main__":
    parser = "another_parser"

    parser_module = importlib.import_module('parsers.{}'.format(parser))
    process = getattr(parser_module, 'process')
    process()