import pkgutil
import importlib

__all__ = []

# Recorre todos los módulos dentro del paquete `utils`
for module_info in pkgutil.iter_modules(__path__):
    name = module_info.name

    # Ignorar archivos privados y el propio __init__.py
    if name.startswith("_"):
        continue

    # Importa dinámicamente el submódulo
    module = importlib.import_module(f"{__name__}.{name}")

    # Determina qué nombres exportar:
    # - Si el módulo define __all__, usa eso.
    # - Si no, exporta todo lo que no empieza con "_".
    public_names = getattr(module, "__all__", None)
    if public_names is None:
        public_names = [
            n for n in module.__dict__
            if not n.startswith("_")
        ]

    # Agrega las variables/funciones al espacio global de utils
    for n in public_names:
        globals()[n] = getattr(module, n)
    __all__.extend(public_names)
