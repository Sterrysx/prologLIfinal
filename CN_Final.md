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

*(Capítulos 2‑5 se completarán más adelante)*
