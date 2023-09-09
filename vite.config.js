import { resolve } from 'path'

export default {
        // configure our build
	build: {
		lib: {
		// entry point for Vite to interact with elm-worker files
			entry: resolve(__dirname, './elm-pkg-js-vite/ports.js'),
			name: 'process-ports',
			// this will be the name of the output file resulting from build
			formats: ['cjs'],
			fileName: 'ports'
		},
		// do not copy the contents of ./public as it is not needed for the elm-pkg-js
		copyPublicDir: false,
		// clear out the old result in the outDir
		emptyOutDir: true,
		// file path for the build output directory
		outDir: "./elm-pkg-js",
		// esbuild target
		target: "es2020"
	}
}