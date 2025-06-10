# Hoja de repaso para el examen de MATLAB

## Tabla de contenidos

1. [Transformada de Fourier: conceptos básicos](#1-transformada-de-fourier-conceptos-básicos)
2. [Métodos no lineales](#2-métodos-no-lineales)
3. [Errores numéricos y conceptos puntuales](#3-errores-numéricos-y-conceptos-puntuales)
4. [Derivadas e integrales numéricas](#4-derivadas-e-integrales-numéricas)
5. [Ecuaciones diferenciales ordinarias (EDO)](#5-ecuaciones-diferenciales-ordinarias-edo)

---

## 1. Transformada de Fourier: conceptos básicos

### 1.1 ¿Qué es la transformada de Fourier?

La transformada de Fourier (FT) descompone una señal del dominio del tiempo (o del espacio) en una superposición de senos y cosenos. Convierte la señal al **dominio de la frecuencia**, mostrando qué componentes armónicos la forman y con qué amplitudes.

Matemáticamente, la FT continua de una función $f(t)$ es:

$$
F(\omega) = \int_{-\infty}^{\infty} f(t)\,e^{-i\omega t}\,dt.
$$

En el ordenador se usa la **Transformada Discreta de Fourier (DFT)** sobre $N$ muestras:

$$
X_k = \sum_{n=0}^{N-1} x_n\,e^{-i 2\pi kn/N},\quad k=0,\dots,N-1.
$$

### 1.2 ¿Para qué sirve?

* **Análisis espectral:** detectar frecuencias dominantes (audio, vibraciones, EEG…).
* **Filtrado:** pasar‑banda, notch, etc., multiplicando en frecuencia y volviendo con la IFFT.
* **Compresión:** JPEG usa la DCT (prima de la FT) para concentrar energía en pocos coeficientes.
* **Convolución rápida:** $\text{IFFT}(\text{FFT}(x)\cdot\text{FFT}(h))$ ≡ $x*h$ pero en $\mathcal{O}(N\log N)$.
* **Métodos espectrales:** resolver PDEs (flujo, calor, ondas) representando derivadas en frecuencia.

### 1.3 FT vs DFT vs FFT

| Transformada | Dominio          | Fórmula directa                            | Complejidad                |
| ------------ | ---------------- | ------------------------------------------ | -------------------------- |
| FT continua  | Tiempo continuo  | Integral                                   | —                          |
| DFT          | Muestras finitas | Suma doble                                 | $\mathcal{O}(N^2)$         |
| **FFT**      | Idem DFT         | Algoritmo recursivo Cooley–Tukey (u otros) | **$\mathcal{O}(N\log N)$** |

> **FFT = *cómo* se calcula la DFT de forma eficiente; no es una transformada distinta.**

### 1.4 Conceptos clave para el examen

* **Teorema de muestreo (Nyquist):** evitar aliasing; frecuencia de muestreo $f_s > 2f_{max}$.
* **Alias:** dos señales diferentes producen la misma muestra cuando $f$ $> f_s/2$.
* **Ventaneo:** reducir fuga espectral multiplicando la señal por ventanas (Hann, Hamming…).
* **Twiddle factors:** términos $e^{-i2\pi kn/N}$ usados al recombinar sub‑DFTs en la FFT.
* **Zero‑padding:** añadir ceros para densificar el eje de frecuencias (mejor interpolación, NO mayor resolución).

### 1.5 Preguntas tipo “bola”

1. **¿Qué relación hay entre convolución en tiempo y multiplicación en frecuencia?**  $\mathcal{F}\{x*h\} = X\cdot H$.
2. **¿Por qué la FFT es $\mathcal{O}(N\log N)$ y no $\mathcal{O}(N^2)$?** Divide la DFT en subproblemas de mitad de tamaño y los combina.
3. **Explica aliasing con un ejemplo.**  Un tono de 9 kHz muestreado a 10 kHz se replica como 1 kHz porque supera $f_s/2$.
4. **¿Qué hace `fftshift` en MATLAB?**  Reordena el vector FFT para que 0 Hz quede en el centro.
5. **¿Para qué sirve `abs(fft(x))`?**  Magnitud del espectro; muestra la energía por frecuencia.

### 1.6 Ejemplo MATLAB: FFT de un audio propio

```matlab
% Lee el archivo de audio
audioFile = 'myVoice.wav';
[x, fs] = audioread(audioFile);
N = length(x);

% Cálculo de la FFT
X = fft(x);
f = (0:N-1) * (fs/N);      % eje de frecuencias

% Representación del espectro
audioName = erase(audioFile, '.wav');
figure;
plot(f, abs(X));
xlabel('Frecuencia (Hz)'); ylabel('|X(f)|');
title(['Espectro de ', audioName]);

% Filtrado simple (ejemplo: pasa bajos a 4 kHz)
f_cut = 4000;              % Hz
H = double(f < f_cut);
Xf = X .* H;               % atenuar altas frecuencias
xf = real(ifft(Xf));       % señal filtrada en el tiempo

audiowrite('voice_filtered.wav', xf, fs);
```

**Puntos que recordar**

* `fft` asume la señal periódica; si hay «clicks», aplicar ventana.
* Comparar la versión original y filtrada usando `sound(x,fs)` y `sound(xf,fs)`.
* Para espectro centrado, usar `fftshift` y crear eje `linspace(-fs/2, fs/2, N)`.

---

## 2. Métodos no lineales

> En esta sección presentamos una pequeña **cheatsheet** con los métodos de búsqueda de raíces estudiados, sus fórmulas clave y a continuación los scripts MATLAB que resuelven cada ejercicio.

### 2.1 Método gráfico de puntos fijos

* **Objetivo:** visualizar la intersección de $y=\cos(x)$ y $y=x$ como paso introductorio al método de puntos fijos.
* **Idea:** definir la función auxiliar $ϕ(x)=\cos(x)$, graficar

  1. $y=\cos(x)$
  2. $y=\cos(x)-x$
  3. $y=x-\cos(x)$
     para ver dónde se cruzan.

```matlab
% Ejercicio 1: soluciones gráficas de f(x) = cos(x) - x
data = 0:0.01:1;
figure;
subplot(3,1,1), plot(data, cos(data)), title('y = cos(x)'), grid on;
subplot(3,1,2), plot(data, cos(data)-data), title('y = cos(x) - x'), grid on;
subplot(3,1,3), plot(data, data - cos(data)), title('y = x - cos(x)'), grid on;
```

---

### 2.2 Método de la Bisección

* **Ecuación:** encontrar raíz de $f(x)=x^3 + 3x + 1$ en $[a,b]=[0,1]$.
* **Fórmula de paso:**

  $$
    m = \frac{a + b}{2},
  $$

  escoge el subintervalo $[a,m]$ o $[m,b]$ donde cambie de signo $f(a)f(m)<0$.
* **Precisión:** detiene cuando $|b - a| < \varepsilon$.

```matlab
% Ejercicio 2: bisección para f(x) = x.^3 + 3*x + 1 en [0,1]
f = @(x) x.^3 + 3.*x + 1;
a = 0; b = 1;
epsilon = 1e-1;  % precisión
L = abs(b - a);
n_max = ceil(log(L/epsilon)/log(2) + 1);
i = 0;
while abs(b - a) > epsilon && i < n_max
    i = i + 1;
    m = (a + b)/2;
    if f(a)*f(m) < 0
        b = m;
    else
        a = m;
    end
    fprintf('Iter %d: a=%.5f, b=%.5f, m=%.5f, f(m)=%.5f\n', i, a, b, m, f(m));
end
root = (a + b)/2;
fprintf('Raíz aproximada: x=%.5f, f(x)=%.5f\n', root, f(root));

% Visualización
dir = linspace(0,1,100);
figure;
plot(dir, f(dir), 'b'); hold on;
yline(0,'--r');
plot(root, f(root),'xr','LineWidth',2,'MarkerSize',8);
title('Bisección: f(x) = x^3 + 3x + 1'); grid on; hold off;
```

---

### 2.3 Bisección para $x\,\ln(x) - 2$

* **Ecuación:** $f(x)=x\ln(x)-2$ en $[2,3]$.
* **Misma fórmula** de bisección y criterio $|b-a|<10^{-2}$.

```matlab
% Ejercicio 3: bisección para f(x) = x.*log(x) - 2 en [2,3]
f = @(x) x.*log(x) - 2;
a = 2; b = 3;
epsilon = 1e-2;  % precisión
while abs(b - a) > epsilon
    m = (a + b)/2;
    if f(a)*f(m) < 0
        b = m;
    else
        a = m;
    end
end
root = (a + b)/2;
f_val = f(root);
fprintf('Raíz aproximada: x=%.5f, f(x)=%.5f\n', root, f_val);
```

---

### 2.4 Método de la Regula Falsi (Falsa posición)

* **Ecuación base:** $f(x)=x^3 - 3x + 1$ en $[0,1]$.
* **Fórmula de iteración:**

  $$
    x_{0} = \frac{a\,f(b) - b\,f(a)}{f(b) - f(a)},
  $$

  luego reemplaza $a$ o $b$ según el signo de $f(a)f(x_0)$.
* **Criterios:** $|b - a|<\varepsilon_1, |f(x_0)|<\varepsilon_2$.

```matlab
% Ejercicio 4: Regula Falsi para f(x) = x.^3 - 3*x + 1 en [0,1]
f = @(x) x.^3 - 3.*x + 1;
a = 0; b = 1;
epsilon1 = 1e-5;  % intervalo
epsilon2 = 1e-5;  % valor de f
x0 = (a*f(b) - b*f(a)) / (f(b) - f(a));
i = 0;
while abs(b - a) > epsilon1 && abs(f(x0)) > epsilon2
    i = i + 1;
    if f(a)*f(x0) < 0
        b = x0;
    else
        a = x0;
    end
    x0 = (a*f(b) - b*f(a)) / (f(b) - f(a));
    fprintf('Iter %d: a=%.5f, b=%.5f, x0=%.5f, f(x0)=%.5f\n', i, a, b, x0, f(x0));
end
fprintf('Regula Falsi raíz: x=%.5f, f(x)=%.5f\n', x0, f(x0));
```

---

### 2.5 Método de Newton–Raphson

* **Ecuación:** $f(x)=x^3 - 3x + 1$, derivada $f'(x)=3x^2 - 3$.
* **Iteración:**

  $$
    x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}.
  $$
* **Criterios:** $|\Delta x|<\varepsilon_1, |f(x_n)|<\varepsilon_2$.

```matlab
% Ejercicio 5: Newton-Raphson para f(x) = x.^3 - 3*x + 1 en [0,1]
f = @(x) x.^3 - 3.*x + 1;
df = @(x) 3.*x.^2 - 3;
x = 0.5;         % punto inicial
epsilon1 = 1e-5; % criterio Δx
epsilon2 = 1e-5; % criterio f(x)
i = 0;
while true
    x_new = x - f(x)/df(x);
    i = i + 1;
    if abs(x_new - x) < epsilon1 || abs(f(x_new)) < epsilon2
        x = x_new;
        break;
    end
    x = x_new;
end
fprintf('Newton-Raphson raíz: x=%.5f, f(x)=%.5f, iter=%d\n', x, f(x), i);
```

---

### 2.6 Método de la secante

* **Ecuación:** $f(x)=x^3 - 3x + 1$.
* **Iteración:**

  $$
    x_{n+1} = x_n - f(x_n)\frac{x_n - x_{n-1}}{f(x_n) - f(x_{n-1})}.
  $$
* **Criterio:** $|x_{n+1} - x_n| < \varepsilon$.

```matlab
% Ejercicio 6: método de la secante para f(x) = x.^3 - 3*x + 1 en [1,2]
f = @(x) x.^3 - 3.*x + 1;
x0 = 1; x1 = 2;
epsilon = 1e-2; % precisión en Δx
i = 0;
while abs(x1 - x0) > epsilon
    x2 = x1 - f(x1)*(x1 - x0)/(f(x1) - f(x0));
    x0 = x1;
    x1 = x2;
    i = i + 1;
end
fprintf('Secante raíz: x=%.5f, f(x)=%.5f, iter=%d\n', x1, f(x1), i);
```

---

