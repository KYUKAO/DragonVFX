using UnityEngine;
using UnityEditor;

public class DecompressMaterials : MonoBehaviour
{
    [MenuItem("Custom/Decompress Materials")]
    private static void DecompressSelectedMaterials()
    {
        // 获取当前选中的所有物体
        GameObject[] selectedObjects = Selection.gameObjects;

        foreach (GameObject obj in selectedObjects)
        {
            // 获取当前物体及其一级子物体的所有 SkinnedMeshRenderer 组件
            SkinnedMeshRenderer[] renderers = obj.GetComponentsInChildren<SkinnedMeshRenderer>(includeInactive: true);

            foreach (SkinnedMeshRenderer renderer in renderers)
            {
                // 获取 Mesh 的导入设置
                ModelImporter modelImporter = AssetImporter.GetAtPath(AssetDatabase.GetAssetPath(renderer.sharedMesh)) as ModelImporter;

                // 设置 Mesh 的 Materials 选项为 Use External Materials，并且设置 Location 和 Naming
                modelImporter.materialImportMode = ModelImporterMaterialImportMode.ImportViaMaterialDescription;
                modelImporter.materialLocation = ModelImporterMaterialLocation.External;
                modelImporter.materialName = ModelImporterMaterialName.BasedOnModelNameAndMaterialName;

                // 应用更改到 Mesh 文件上
                AssetDatabase.ImportAsset(AssetDatabase.GetAssetPath(renderer.sharedMesh));
            }
        }

        Debug.Log("Materials decompressed for selected objects and their children.");
    }
}