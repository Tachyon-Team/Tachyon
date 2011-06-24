// Execute the backend unit tests
tests.x86.encoding();
tests.x86.codegen();

// Initialize the Tachyon configuration
initConfig(PLATFORM_64BIT, 'all');

// Create a backend interface instance
var backend = new x86.Backend(PLATFORM_64BIT);










print('done');
