#include <stdlib.h>
#include <stdio.h>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>

#define NB_TEXTURES 5

typedef struct s_pixel *Pixel;

struct s_pixel
{
  Uint8 r, g, b;
};

Pixel newPixel(Uint8 r, Uint8 g, Uint8 b)
{
  Pixel pix = NULL;
  pix = malloc(sizeof(struct s_pixel));
  pix->r = r;
  pix->g = g;
  pix->b = b;

  return pix;
}

Uint32 getUint32(SDL_Surface *surface, int x, int y)
{
  int bpp = surface->format->BytesPerPixel;
  /* Here p is the address to the pixel we want to retrieve */
  Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

  switch(bpp) {
  case 1:
    return *p;
    break;

  case 2:
    return *(Uint16 *)p;
    break;

  case 3:
    if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
      return p[0] << 16 | p[1] << 8 | p[2];
    else
      return p[0] | p[1] << 8 | p[2] << 16;
    break;

  case 4:
    return *(Uint32 *)p;
    break;

  default:
    return 0;       /* shouldn't happen, but avoids warnings */
  }
}

void putUint32(SDL_Surface *surface, int x, int y, Uint32 pixel)
{
  int bpp = surface->format->BytesPerPixel;
  /* Here p is the address to the pixel we want to set */
  Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

  switch(bpp) {
  case 1:
    *p = pixel;
    break;

  case 2:
    *(Uint16 *)p = pixel;
    break;

  case 3:
    if(SDL_BYTEORDER == SDL_BIG_ENDIAN) {
      p[0] = (pixel >> 16) & 0xff;
      p[1] = (pixel >> 8) & 0xff;
      p[2] = pixel & 0xff;
    } else {
      p[0] = pixel & 0xff;
      p[1] = (pixel >> 8) & 0xff;
      p[2] = (pixel >> 16) & 0xff;
    }
    break;

  case 4:
    *(Uint32 *)p = pixel;
    break;
  }
}

Pixel getPixel(SDL_Surface *image, int x, int y)
{
  Uint32 pix32;
  Uint8 r, g, b;

  pix32 = getUint32(image, x, y);
  SDL_GetRGB(pix32, image->format, &r, &g, &b);

  return newPixel(r, g, b);
}

void putPixel(SDL_Surface *image, int x, int y, Pixel pixel)
{
  Uint32 pix32;
  pix32 = SDL_MapRGB(image->format, pixel->r, pixel->g, pixel->b);
  putUint32(image, x, y, pix32);
}

void generateTextureRate(float *rate, Uint8 height)
{
  float step[] = { 20.0, 50.0, 80.0, 110.0, 140.0, 170.0, 200.0, 230.0 };

  if (height < step[0])
  {
    rate[0] = 1.0;
    rate[1] = 0.0;
    rate[2] = 0.0;
    rate[3] = 0.0;
    rate[4] = 0.0;
  }
  else if(height < step[1])
  {
    rate[0] = 1.0 - (height - step[0]) / (step[1] - step[0]);
    rate[1] = (height - step[0]) / (step[1] - step[0]);
    rate[2] = 0.0;
    rate[3] = 0.0;
    rate[4] = 0.0;
  }
  else if(height < step[2])
  {
    rate[0] = 0.0;
    rate[1] = 1.0;
    rate[2] = 0.0;
    rate[3] = 0.0;
    rate[4] = 0.0;
  }
  else if(height < step[3])
  {
    rate[0] = 0.0;
    rate[1] = 1.0 - (height - step[2]) / (step[3] - step[2]);
    rate[2] = (height - step[2]) / (step[3] - step[2]);
    rate[3] = 0.0;
    rate[4] = 0.0;
  }
  else if(height < step[4])
  {
    rate[0] = 0.0;
    rate[1] = 0.0;
    rate[2] = 1.0;
    rate[3] = 0.0;
    rate[4] = 0.0;
  }
  else if(height < step [5])
  {
    rate[0] = 0.0;
    rate[1] = 0.0;
    rate[2] = 1.0 - (height - step[4]) / (step[5] - step[4]);
    rate[3] = (height - step[4]) / (step[5] - step[4]);
    rate[4] = 0.0;
  }
  else if(height < step[6])
  {
    rate[0] = 0.0;
    rate[1] = 0.0;
    rate[2] = 0.0;
    rate[3] = 1.0;
    rate[4] = 0.0;
  }
  else if(height < step[7])
  {
    rate[0] = 0.0;
    rate[1] = 0.0;
    rate[2] = 0.0;
    rate[3] = 1.0 - (height - step[6]) / (step[7] - step[6]);
    rate[4] = (height - step[6]) / (step[7] - step[6]);
  }
  else
  {
    rate[0] = 0.0;
    rate[1] = 0.0;
    rate[2] = 0.0;
    rate[3] = 0.0;
    rate[4] = 1.0;
  }
}

void generateTexture(float *rate, Pixel pix, Pixel *pixTex)
{
  int i;

  pix->r = 0;

  for (i = 0; i < NB_TEXTURES; ++i)
    pix->r += rate[i] * pixTex[i]->r;

  pix->g = 0;

  for (i = 0; i < NB_TEXTURES; ++i)
    pix->g += rate[i] * pixTex[i]->g;

  pix->b = 0;

  for (i = 0; i < NB_TEXTURES; ++i)
    pix->b += rate[i] * pixTex[i]->b;
}

Uint8 getRandHeight(Uint8 height)
{
  int h = height + (rand() % 30) - 15;

  if (h < 0)
    h = 0;

  if (h > 255)
    h = 255;

  return h;
}

void generate(char* str)
{
  SDL_Surface *image = NULL, *imgtmp, *imgTex[NB_TEXTURES];
  SDL_Rect positionImage;
  int i, j, h;
  float rate[NB_TEXTURES];
  Pixel pix, pixTex[NB_TEXTURES];

  SDL_Init(SDL_INIT_VIDEO);

  positionImage.x = 0;
  positionImage.y = 0;

  image = IMG_Load(str);

  imgTex[0] = IMG_Load("Textures/underwater.jpg");
  imgTex[1] = IMG_Load("Textures/water.jpg");
  imgTex[2] = IMG_Load("Textures/grass.jpg");
  imgTex[3] = IMG_Load("Textures/rock.jpg");
  imgTex[4] = IMG_Load("Textures/snow.jpg");

  imgtmp = SDL_CreateRGBSurface(SDL_HWSURFACE, image->w, image->h, 32, 0, 0, 0,
				0);

  SDL_LockSurface(imgtmp);

  for (j = 0; j < imgtmp->h; ++j)
  {
    for (i = 0; i < imgtmp->w; ++i)
    {
      pix = getPixel(image, i, j);

      for (h = 0; h < NB_TEXTURES; ++h)
	pixTex[h] = getPixel(imgTex[h], i % imgTex[h]->w, j % imgTex[h]->h);

      generateTextureRate(rate, getRandHeight(pix->r));
      generateTexture(rate, pix, pixTex);

      putPixel(imgtmp, i, j, pix);

      for (h = 0; h < NB_TEXTURES; ++h)
	free(pixTex[h]);

      free(pix);
    }
  }

  SDL_UnlockSurface(imgtmp);

  SDL_SaveBMP(imgtmp, "generate_texture.bmp");

  for (h = 0; h < NB_TEXTURES; ++h)
    SDL_FreeSurface(imgTex[h]);

  SDL_FreeSurface(imgtmp);
  SDL_FreeSurface(image);

  SDL_Quit();
}
