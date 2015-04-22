
#include <jack/jack.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void COMPILER_ASSERTS();

#include "foo.h"

jack_client_t* client;
jack_port_t *output_port;

int process (jack_nframes_t nframes, void *arg) {
  jack_default_audio_sample_t *out =
    (jack_default_audio_sample_t *) jack_port_get_buffer (output_port, nframes);
  int i;
  for (i = 0; i < nframes; i++) {
    out[i] = jack_ivory_main();
  }
  return 0;
}

int main() {
  printf("starting...\n");
  printf("result: %f\n", jack_ivory_main());
  if ((client = jack_client_open("foo", 0, 0)) == 0) {
    fprintf (stderr, "jack server not running?\n");
    return 1;
  }
  jack_set_process_callback(client, process, 0);
  output_port = jack_port_register
    (client, "output", JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0);
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
  if (jack_connect (client, jack_port_name (output_port), ports[0])) {
    fprintf (stderr, "cannot connect output ports\n");
  }

  // connect to meterbridge
  if ((ports = jack_get_ports(client, "bridge", NULL, JackPortIsInput)) == NULL) {
    fprintf(stderr, "Cannot find meterbridge\n");
  } else {
    if (jack_connect (client, jack_port_name (output_port), ports[0])) {
      fprintf (stderr, "cannot connect output ports\n");
    }
  }

  sleep(1);
  jack_client_close(client);
  return 0;
}
