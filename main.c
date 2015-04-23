
#include <jack/jack.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void COMPILER_ASSERTS();

#include "foo.h"

jack_client_t* client;
jack_port_t *output_port_left;
jack_port_t *output_port_right;

int process (jack_nframes_t nframes, void *arg) {
  jack_default_audio_sample_t *out_left =
    (jack_default_audio_sample_t *) jack_port_get_buffer(output_port_left, nframes);
  jack_default_audio_sample_t *out_right =
    (jack_default_audio_sample_t *) jack_port_get_buffer(output_port_right, nframes);
  int i;
  float out;
  for (i = 0; i < nframes; i++) {
    out = jack_ivory_main();
    out_left[i] = out;
    out_right[i] = out;
  }
  return 0;
}

int main() {
  printf("starting...\n");
  if ((client = jack_client_open("foo", 0, 0)) == 0) {
    fprintf (stderr, "jack server not running?\n");
    return 1;
  }
  jack_set_process_callback(client, process, 0);
  output_port_left = jack_port_register
    (client, "output_left", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
  output_port_right = jack_port_register
    (client, "output_right", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
  if (jack_activate(client)) {
    fprintf (stderr, "cannot activate client");
    return 1;
  }

  // connect to physical outputs
  const char **ports;
  if ((ports = jack_get_ports(client, NULL, NULL, JackPortIsPhysical|JackPortIsInput)) == NULL) {
    fprintf(stderr, "Cannot find any physical playback ports\n");
    exit(1);
  }
  if (jack_connect(client, jack_port_name(output_port_left), ports[0])) {
    fprintf (stderr, "cannot connect left output port\n");
  }
  if (jack_connect(client, jack_port_name(output_port_right), ports[1])) {
    fprintf (stderr, "cannot connect right output port\n");
  }

  // connect to meterbridge
  if ((ports = jack_get_ports(client, "bridge", NULL, JackPortIsInput)) == NULL) {
    fprintf(stderr, "Cannot find meterbridge\n");
  } else {
    if (jack_connect(client, jack_port_name (output_port_left), ports[0])) {
      fprintf (stderr, "cannot connect left port to meterbridge\n");
    }
    if (jack_connect(client, jack_port_name (output_port_right), ports[1])) {
      fprintf (stderr, "cannot connect right port to meterbridge\n");
    }
  }

  sleep(10);
  jack_client_close(client);
  return 0;
}
