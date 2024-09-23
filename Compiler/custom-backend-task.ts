import { Dirent } from 'node:fs';
import { stat, readdir } from 'node:fs/promises';

export async function readFolder(path: string): Promise<Dirent[]> {
    return readdir(path, { withFileTypes: true, recursive: true })
        .then(files => {
            let length: number = files.length
            let i: number = 0

            while (i < length) {
                if (files[i].isFile()) {
                    i += 1
                } else {
                    files[i] = files[length--]
                }
            }

            files.length = length

            return files
        })
}

