namespace Todonero
{

	using System.Collections.Generic;
	using UnityEngine;

	public class Demo : MonoBehaviour
	{

		[SerializeField] private List<ParticleSystem> _particles = new List<ParticleSystem>();
		[SerializeField] private List<Transform> transforms = new List<Transform>();
		[ColorUsage(true, true)] public Color color;

		private List<ParticleSystemRenderer> _particleSystemRenderers = new List<ParticleSystemRenderer>();

		private void Start()
		{
			Application.targetFrameRate = 120;
			foreach (var childTransform in transforms)
			{
				foreach (Transform child in childTransform)
				{
					if (child.GetComponent<ParticleSystemRenderer>() == null)
					{
						return;
					}

					if (child.GetComponent<ParticleSystemRenderer>().material != null)
					{
						child.GetComponent<ParticleSystemRenderer>().material.SetColor("_color", color);
						_particleSystemRenderers.Add(child.GetComponent<ParticleSystemRenderer>());
					}

					if (child.GetComponent<ParticleSystemRenderer>().trailMaterial != null)
					{
						child.GetComponent<ParticleSystemRenderer>().trailMaterial.SetColor("_color", color);
						_particleSystemRenderers.Add(child.GetComponent<ParticleSystemRenderer>());
					}
				}
			}
		}

		private void Update()
		{
			if (Input.GetKey(KeyCode.Space))
			{
				SetColor();
				foreach (var particle in _particles)
				{
					particle.Play();
				}
			}
		}

		private void SetColor()
		{
			foreach (var _particleSystemRenderer in _particleSystemRenderers)
			{
				if (_particleSystemRenderer.GetComponent<ParticleSystemRenderer>().material != null)
				{
					_particleSystemRenderer.GetComponent<ParticleSystemRenderer>().material.SetColor("_color", color);
				}

				if (_particleSystemRenderer.GetComponent<ParticleSystemRenderer>().trailMaterial != null)
				{
					_particleSystemRenderer.GetComponent<ParticleSystemRenderer>().trailMaterial.SetColor("_color", color);
				}
			}
		}
	}
}
